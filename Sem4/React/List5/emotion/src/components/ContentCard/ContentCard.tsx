export default function ContentCard({ children }: { children: React.ReactNode }) {
    return (
        <div className="content-card">
            {children}
        </div>
    );
}